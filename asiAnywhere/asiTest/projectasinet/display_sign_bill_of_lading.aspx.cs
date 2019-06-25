
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
using System.Data.SqlClient;
using System.IO;
using System.Web;
#endregion

public partial class display_sign_bill_of_lading : System.Web.UI.Page
{

    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        //ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "display_sign_bill_of_lading.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            labelcompany.Text = PrmComp;
            Session["Customers_Company"] = labelcompany.Text;
           
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
            if (!Page.IsPostBack)
            {
                           
               
                if (Session["User"] != null)
                {
                    //UserClass UserLogin = (UserClass)Session["User"]; 
                    lblUser.Text = UserLogin.UserName;

                }
                TextBox6.Text = "99999999";
                CheckBox1.Checked = true;
                CheckBox2.Checked = true;
                CheckBox4.Checked = true;

               

            }
            

            
        }




        string sCulture = ConfigurationManager.AppSettings["LCID"];
        if (!String.IsNullOrEmpty(sCulture))
        {
            int nCulture = int.Parse(sCulture);
            System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo(nCulture, false);
        }

               
    }

    protected void hlnkLogOut_Click(object sender, EventArgs e)
    {

        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        Response.Redirect(sLoginURL);
    }

    protected void Back_tomenu_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }

    protected void submitbutton_click(object sender, EventArgs e)
    {
        string str1 = "";
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        if (CheckBox1.Checked)
            HiddenField1.Value = "Yes";
        else
            HiddenField1.Value = "No";

        if (CheckBox2.Checked)
            HiddenField2.Value = "Yes";
        else
            HiddenField2.Value = "No";

        //if (CheckBox3.Checked)
        //    HiddenField3.Value = "Yes";
        //else
        //    HiddenField3.Value = "No";

        if (CheckBox4.Checked)
            HiddenField4.Value = "Yes";
        else
            HiddenField4.Value = "No";
        
        //string str1 = Path.GetFileName(imagecontrol);

        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from sign_bol where cust = '" + TextBox1.Text.Trim() + "' and bol = '" + TextBox3.Text.Trim() + "' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);
            conn.Close();
                        
            str1 = ds.Tables[0].Rows[0][2].ToString();
         }
        catch
        {
            //Label1.Text = "Error :" + ex.Message + "<p>";
            conn.Close();
        }
        finally
        {
            conn.Close();
        }
        if (str1 == "")
        {           
            HttpContext.Current.Response.Write("<script> alert('BOL is not Signed')</script>");
            return;
        }

        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Print";
        ObjectDataSource1.SelectParameters["prmcustno"].DefaultValue = TextBox1.Text.Trim();
        ObjectDataSource1.SelectParameters["prmbolno"].DefaultValue = TextBox3.Text.Trim();

        ObjectDataSource1.SelectParameters["imagepath"].DefaultValue = str1;

        ObjectDataSource1.SelectParameters["prmprinted"].DefaultValue = HiddenField1.Value;
        ObjectDataSource1.SelectParameters["prmposted"].DefaultValue = HiddenField2.Value;
        ObjectDataSource1.SelectParameters["prmpostbol"].DefaultValue = HiddenField4.Value;

        ObjectDataSource1.SelectParameters["prmBegOrder"].DefaultValue = TextBox5.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndOrder"].DefaultValue = TextBox6.Text.Trim();

        try
        {
            Label vpath = (Label)FormView1.FindControl("signLabel");
            if (vpath.Text != "")
            {
                string path = vpath.Text;
                string path2 = @"/pdfs/" + path;
                Session["Bill_of_led_list_sign_bol"] = path2;
                if (path2 != "")
                {
                    if (!Request.Browser.Browser.Contains("Safari"))
                        Response.Write("<script>var win=window.open('print_sign_bill_lading.aspx'); target='_blank'; if(win==null || win=='undefined') alert('Popup has Blocked this page. To open this page enable popups.');</script>");
                    else
                        Response.Redirect("display_sign_bill_of_lading.aspx");
                }
            }
            else
            {
                Label1.Text = "No Pdf Exists";
            }
        }
        catch
        {
            Response.Redirect("display_sign_bill_of_lading.aspx");
        }
        finally
        {
            Response.Write("<script>window.location.href='display_sign_bill_of_lading.aspx'</script>");
        }
        

       
    }
   
   
    protected void FormView1_PreRender(object sender, EventArgs e)
    {
       
    }
   
}
