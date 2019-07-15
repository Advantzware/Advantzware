
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

public partial class bill_of_lading_report : System.Web.UI.Page
{

    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        //ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "bill_of_lading_report.aspx";
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
                ObjectDataSource2.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource2.SelectParameters["prmComp"].DefaultValue = Convert.ToString(Session["Customers_Company"]);
                
                 SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                 try
                 {
                     conn.Open();

                     string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'bill_of_lading_report.aspx'  ";
                     SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                     DataSet ds = new DataSet();
                     da.Fill(ds);
                     foreach (DataRow dr in ds.Tables[0].Rows)
                     {
                         
                         if (aUsers == "internal")
                         {
                             TextBox1.Text = dr["field1"].ToString();
                             TextBox2.Text = dr["field2"].ToString();
                         }
                         if (aUsers == "external")
                         {
                             try
                             {
                                 string UserId = UserLogin.UserName;
                                 string aDefaultCust = null;
                                 string aComp = null;

                                 func1 user = new func1();
                                 user.CheckUserCustomer(aComp, UserId, ref  aDefaultCust);
                                 TextBox1.Text = aDefaultCust;
                                 TextBox2.Text = aDefaultCust;
                             }
                             catch { }                             
                         }
                         TextBox3.Text = dr["field3"].ToString();
                         TextBox4.Text = dr["field4"].ToString();
                         TextBox5.Text = dr["field5"].ToString();
                         TextBox6.Text = dr["field6"].ToString();

                         if (dr["rd_field1"].ToString() == "BOL")
                             RadioButtonList1.SelectedIndex = 0;
                         if (dr["rd_field1"].ToString() == "Certificate Of Compliance")
                             RadioButtonList1.SelectedIndex = 1;


                         if (dr["chk_field1"].ToString() == "Yes")
                             CheckBox1.Checked = true;
                         else
                             CheckBox1.Checked = false;

                         if (dr["chk_field2"].ToString() == "Yes")
                             CheckBox2.Checked = true;
                         else
                             CheckBox2.Checked = false;

                         if (dr["chk_field3"].ToString() == "Yes")
                             CheckBox4.Checked = true;
                         else
                             CheckBox4.Checked = false;                                                 
                         
                         if (dr["chk_field5"].ToString() == "Yes")
                             RePrintCheckBox.Checked = true;
                         else
                             RePrintCheckBox.Checked = false;

                         if (dr["chk_field6"].ToString() == "Yes")
                             CheckBox_post.Checked = true;
                         else
                             CheckBox_post.Checked = false;
                     }
                 }
                 catch { }

                if(TextBox4.Text == "")
                TextBox4.Text = "99";
                if(TextBox6.Text == "")
                TextBox6.Text = "99999999";
            if (TextBox1.Text == "" && TextBox2.Text == "")
            {
                try
                {
                    Label begin = (Label)FormView2.FindControl("CustLabel");
                    TextBox1.Text = begin.Text;
                    TextBox2.Text = begin.Text;
                }
                catch { }
            }

                if (Session["User"] != null)
                {
                    //UserClass UserLogin = (UserClass)Session["User"]; 
                    lblUser.Text = UserLogin.UserName;

                }

                if (aUsers == "external")
                {
                    TextBox2.ReadOnly = true;

                }
                if (aUsers == "internal")
                {
                    //TextBox2.ReadOnly = true;

                }

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

        if (PostedCheckBox.Checked)
            HiddenField9.Value = "Yes";
        else
            HiddenField9.Value = "No";

        if (RePrintCheckBox.Checked)
            HiddenField10.Value = "Yes";
        else
            HiddenField10.Value = "No";

        if (CheckBox_post.Checked)
            HiddenField11.Value = "Yes";
        else
            HiddenField11.Value = "No";


        

        if (RadioButtonList1.SelectedIndex == 0)
            HiddenField7.Value = "BOL";
        if (RadioButtonList1.SelectedIndex == 1)
            HiddenField7.Value = "Certificate Of Compliance";

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "bill_of_lading_report.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            if (aUsers == "external")
            {
                ObjectDataSource1.SelectParameters["prmEndCust"].DefaultValue = TextBox1.Text.Trim();

            }

            if (aUsers == "internal")
            {
                ObjectDataSource1.SelectParameters["prmEndCust"].DefaultValue = TextBox2.Text.Trim();

            }
        }
        
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;        
        ObjectDataSource1.SelectParameters["prmBillAct"].DefaultValue = "BillofLad";
        ObjectDataSource1.SelectParameters["prmBeginCust"].DefaultValue = TextBox1.Text.Trim();         
        ObjectDataSource1.SelectParameters["prmBeBoll"].DefaultValue = TextBox3.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndBoll"].DefaultValue = TextBox4.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBeOrder"].DefaultValue = TextBox5.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndOrder"].DefaultValue = TextBox6.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBillofLad"].DefaultValue = HiddenField1.Value;

        ObjectDataSource1.SelectParameters["prmPallet"].DefaultValue = HiddenField2.Value;
        ObjectDataSource1.SelectParameters["prmPostedBol"].DefaultValue = HiddenField10.Value;
        ObjectDataSource1.SelectParameters["prmPackList"].DefaultValue = HiddenField4.Value;
        ObjectDataSource1.SelectParameters["prmPrint"].DefaultValue = HiddenField7.Value;
        ObjectDataSource1.SelectParameters["prmBolPost"].DefaultValue = HiddenField9.Value;

        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'bill_of_lading_report.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name, field1, field2, field3, field4, field5, field6, rd_field1, chk_field1, chk_field2, chk_field3,chk_field4, chk_field5,chk_field6) values ('" + UserLogin.UserName + "','bill_of_lading_report.aspx','" + TextBox1.Text.Trim() + "','" + TextBox2.Text.Trim() + "','" + TextBox3.Text.Trim() + "','" + TextBox4.Text.Trim() + "','" + TextBox5.Text.Trim() + "','" + TextBox6.Text.Trim() + "','" + HiddenField7.Value + "','" + HiddenField1.Value + "','" + HiddenField2.Value + "','" + HiddenField4.Value + "','" + HiddenField9.Value + "','" + HiddenField10.Value + "','" + HiddenField11.Value + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + TextBox1.Text.Trim() + "', field2 = '" + TextBox2.Text.Trim() + "', field3 = '" + TextBox3.Text.Trim() + "', field4 = '" + TextBox4.Text.Trim() + "', field5 = '" + TextBox5.Text.Trim() + "', field6 = '" + TextBox6.Text.Trim() + "',rd_field1 = '" + HiddenField7.Value + "', chk_field1 = '" + HiddenField1.Value + "', chk_field2 = '" + HiddenField2.Value + "', chk_field3 = '" + HiddenField4.Value + "', chk_field4 = '" + HiddenField9.Value + "' , chk_field5 = '" + HiddenField10.Value + "', chk_field6 = '" + HiddenField11.Value + "' where user_name = '" + UserLogin.UserName + "' and prog_name ='bill_of_lading_report.aspx' ", conn);
                cmd_update.ExecuteNonQuery();
            }

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
            Label vpath = (Label)FormView1.FindControl("vbillofladLabel");

            if (vpath.Text != "")
            {
                string path = vpath.Text;
                string path2 = @"/pdfs/" + path;
                Session["Bill_of_led_list"] = path2;
                if (path2 != "")
                {
                    if (!Request.Browser.Browser.Contains("Safari"))
                        Response.Write("<script>var win=window.open('print_bill_lading_report.aspx'); target='_blank'; if(win==null || win=='undefined') alert('Popup has Blocked this page. To open this page enable popups.');</script>");
                    else
                        Response.Redirect("bill_of_lading_report.aspx");
                }
            }
            else
            {
                Label1.Text = "No Pdf Exists";
            }
        }
        catch
        {
            Label1.Text = "No Pdf Exists";
        }
        if (Label1.Text == "")
        {
            Response.Write("<script>window.location.href='bill_of_lading_report.aspx'</script>");
        }
    }
   
    protected void FormView1_PreRender(object sender, EventArgs e)
    {
        try
        {
            Label vpath = (Label)FormView1.FindControl("vbillofladLabel");

            if (vpath.Text != "")
            {
                string path = vpath.Text;
                string path2 = @"/pdfs/" + path;
                Session["Bill_of_led_list"] = path2;
                if (path2 != "")
                {
                    if (!Request.Browser.Browser.Contains("Safari"))
                        Response.Write("<script>var win=window.open('print_bill_lading_report.aspx'); target='_blank'; if(win==null || win=='undefined') alert('Popup has Blocked this page. To open this page enable popups.');</script>");
                    else
                        Response.Redirect("bill_of_lading_report.aspx");
                }
            }
            else
            {
                Label1.Text = "No Pdf Exists";
            }
        }
        catch
        {
            Label1.Text = "No Pdf Exists";
        }
        if (Label1.Text == "")
        {
            Response.Write("<script>window.location.href='bill_of_lading_report.aspx'</script>");
        }
    }
}
