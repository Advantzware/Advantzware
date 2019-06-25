#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
using System.Data.SqlClient;
#endregion

public partial class print_ap_check : System.Web.UI.Page
{

    private bool bSort = true;

    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
       
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "print_ap_check.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            labelcompany.Text = PrmComp;
            Session["Customers_Company"] = PrmComp;
            if (aUsers == "external")
            {
               
            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }
        
        string sCulture = ConfigurationManager.AppSettings["LCID"];
        if (!String.IsNullOrEmpty(sCulture))
        {
            int nCulture = int.Parse(sCulture);
            System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo(nCulture, false);
        }

        if (!Page.IsPostBack)
        {
            OutPutFile.Visible = false;
            HyperLink1.Visible = false;
        
            if (Session["User"] != null)
            {                
                lblUser.Text = UserLogin.UserName;
            }
            

            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            try
            {
                conn.Open();

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'print_ap_check.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                foreach (DataRow dr in ds.Tables[0].Rows)
                {
                    chkdateTextBox.Text = dr["field1"].ToString();
                    bnkcodeTextBox.Text = dr["field2"].ToString();
                    strtchkTextBox.Text = dr["field3"].ToString();
                    begvendTextBox.Text = dr["field4"].ToString();
                    endvendTextBox.Text = dr["field5"].ToString();
                             
                }

                conn.Close();
            }
            catch
            {
                conn.Close();
            }
            finally
            {
                conn.Close();
            }


            try
            {


                chkdateTextBox.Text = System.DateTime.Today.ToShortDateString();                
            }
            catch { }
            
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
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        
        
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmapchek"].DefaultValue = "apchek";
        ObjectDataSource1.SelectParameters["prmChekdate"].DefaultValue = chkdateTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmbnkcode"].DefaultValue = bnkcodeTextBox.Text.Trim();        
        ObjectDataSource1.SelectParameters["prmstrtchek"].DefaultValue = strtchkTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBegVend"].DefaultValue = begvendTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndVend"].DefaultValue = endvendTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = Convert.ToString(HiddenFieldPost.Value);



        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'print_ap_check.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name , field1, field2, field3, field4, field5) values ('" + UserLogin.UserName + "','print_ap_check.aspx' , '" + chkdateTextBox.Text.Trim() + "', '" + bnkcodeTextBox.Text.Trim() + "', '" + strtchkTextBox.Text.Trim() + "', '" + begvendTextBox.Text.Trim() + "', '" + endvendTextBox.Text.Trim() + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + chkdateTextBox.Text.Trim() + "', field2 = '" + bnkcodeTextBox.Text.Trim() + "', field3 = '" + strtchkTextBox.Text.Trim() + "', field4 = '" + begvendTextBox.Text.Trim() + "', field5 = '" + endvendTextBox.Text.Trim() + "' where user_name = '" + UserLogin.UserName + "' and prog_name =  'print_ap_check.aspx' ", conn);
                cmd_update.ExecuteNonQuery();
            }
            conn.Close();
        }
        catch (Exception ex)
        {
            Label1.Text = "Error :" + ex.Message + "<p>";
            conn.Close();
        }
        finally
        {
            conn.Close();
        }


        try
        {
            OutPutFile.Visible = true;
            HyperLink1.Visible = true;
            Label vpath = (Label)FormView1.FindControl("apchekLabel");
            HyperLink1.Text = vpath.Text;            
            HyperLink1.NavigateUrl = @"/pdfs/" + vpath.Text;


            if (vpath.Text == "")
            {
                OutPutFile.Text = "No CSV Exists";
                
            }
        }
        catch {
               }       
        
    }

   }
