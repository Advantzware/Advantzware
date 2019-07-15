
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

public partial class cust_inv_report_list : System.Web.UI.Page
{

    private bool bSort = true;

    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "cust_inv_report.aspx";
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
            if (aUsers == "external")
            {
                if (!Page.IsPostBack)
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

                    SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                    try
                    {
                        conn.Open();

                        string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'cust_inv_report.aspx'  ";
                        SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                        DataSet ds = new DataSet();
                        da.Fill(ds);
                        foreach (DataRow dr in ds.Tables[0].Rows)
                        {                           
                            TextBox3.Text = dr["field3"].ToString();
                            TextBox4.Text = dr["field4"].ToString();
                            
                        }
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
                        OutputLabel.Visible = false;
                        HyperLink1.Visible = false;                      
                        
                        TextBox2.ReadOnly = true;
                        Image9.Visible = false;
                        if(TextBox4.Text == "")
                        TextBox4.Text = "zzzzzzzzzzzzzzz";
                        RadioButtonList8.SelectedIndex = 0;
                    }
                    catch { }

                    if (Session["User"] != null)
                    {
                        //UserClass UserLogin = (UserClass)Session["User"]; 
                        lblUser.Text = UserLogin.UserName;

                    }

                }


            }
            if (aUsers == "internal")
            {
                if (!Page.IsPostBack)
                {
                    //ObjectDataSource2.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                    //ObjectDataSource2.SelectParameters["prmComp"].DefaultValue = Convert.ToString(Session["Customers_Company"]);
                    //Label begin = (Label)FormView2.FindControl("CustLabel");
                    //    TextBox1.Text = begin.Text;

                    SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                    try
                    {
                        conn.Open();

                        string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'cust_inv_report.aspx'  ";
                        SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                        DataSet ds = new DataSet();
                        da.Fill(ds);
                        foreach (DataRow dr in ds.Tables[0].Rows)
                        {
                            TextBox1.Text = dr["field1"].ToString();
                            TextBox2.Text = dr["field2"].ToString();
                            TextBox3.Text = dr["field3"].ToString();
                            TextBox4.Text = dr["field4"].ToString();
                            
                        }
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
                        OutputLabel.Visible = false;
                        HyperLink1.Visible = false;
                        if(TextBox2.Text == "")
                        TextBox2.Text = "zzzzzzzz" ;
                        if(TextBox4.Text == "")
                        TextBox4.Text = "zzzzzzzzzzzzzzz";
                        RadioButtonList8.SelectedIndex = 0;
                    }
                    catch { }

                    if (Session["User"] != null)
                    {
                        //UserClass UserLogin = (UserClass)Session["User"]; 
                        lblUser.Text = UserLogin.UserName;

                    }

                }



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
        if (RadioButtonList8.SelectedIndex == 0)
            HiddenField1.Value = "No";
        if (RadioButtonList8.SelectedIndex == 1)
            HiddenField1.Value = "Yes";

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "cust_inv_report.aspx";
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
                ObjectDataSource1.SelectParameters["vEndCust"].DefaultValue = TextBox1.Text.Trim();
            }

            if (aUsers == "internal")
            {
                ObjectDataSource1.SelectParameters["vEndCust"].DefaultValue = TextBox2.Text.Trim();
            }

        }
        ObjectDataSource1.SelectParameters["vBeginCust"].DefaultValue = TextBox1.Text.Trim();
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Cust";
        ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = HiddenField1.Value;
        ObjectDataSource1.SelectParameters["vEndCust"].DefaultValue = TextBox2.Text.Trim();
        ObjectDataSource1.SelectParameters["vFromlistclass"].DefaultValue = TextBox3.Text.Trim();
        ObjectDataSource1.SelectParameters["vTolistclass"].DefaultValue = TextBox4.Text.Trim();



        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'cust_inv_report.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name, field1, field2,field3,field4) values ('" + UserLogin.UserName + "','cust_inv_report.aspx','" + TextBox1.Text.Trim() + "','" + TextBox2.Text.Trim() + "','" + TextBox3.Text.Trim() + "','" + TextBox4.Text.Trim() + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + TextBox1.Text.Trim() + "', field2 = '" + TextBox2.Text.Trim() + "', field3 = '" + TextBox3.Text.Trim() + "', field4 = '" + TextBox4.Text.Trim() + "' where user_name = '" + UserLogin.UserName + "' and prog_name ='cust_inv_report.aspx' ", conn);
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

        if (RadioButtonList8.SelectedIndex == 1)
        {

            try
            {
                OutputLabel.Visible = true;
                HyperLink1.Visible = true;
                Label path = (Label)FormView1.FindControl("vFileLabel");
                HyperLink1.Text = path.Text;
                HyperLink1.NavigateUrl = @"/pdfs/" + path.Text;

                if (path.Text == "")
                {
                    Label1.Text = "No Csv Exists";
                    Response.Write("<script>window.location.href='cust_inv_report.aspx'</script>");
                }
            }
            catch
            {

            }
        }

        if (RadioButtonList8.SelectedIndex == 0)
        {


            try
            {
                Label vpath = (Label)FormView1.FindControl("vFileLabel");

                if (vpath.Text != "")
                {

                    string path = vpath.Text;
                    string path2 = @"/pdfs/" + path;
                    Session["open_cust_inv"] = path2;
                    if (path2 != "")
                    {
                        if (!Request.Browser.Browser.Contains("Safari"))
                        {
                            Response.Write("<script>var win=window.open('print_cust_inv.aspx'); target='_blank'</script>");
                            Response.Write("<script>if(win==null || typeof(win)=='undefined'); alert('PopUp has blocked the page. To see it properly enable popups')</script>");
                        }
                        else
                            Response.Redirect("cust_inv_report.aspx");
                    }
                }
                else
                {
                    Label1.Text = "No Pdf Exists";
                }
                //Response.Write("<script>window.location.href='cust_inv_report.aspx'</script>");
            }
            catch
            {
                //Label1.Text = "No Pdf Exists";
            }

            Response.Write("<script>window.location.href='cust_inv_report.aspx'</script>");
        }

    }
    protected void FormView1_PreRender(object sender, EventArgs e)
    {
        try
        {
            Label vpath = (Label)FormView1.FindControl("vFileLabel");
            // Response.Write(vpath.Text);
            if (vpath.Text != "")
            {
                string path = vpath.Text;
                string path2 = @"/pdfs/" + path;
                if (path2 != "")
                {
                    if (!Request.Browser.Browser.Contains("Safari"))
                        Response.Write("<script>window.open('print_cust_inv.aspx'); target='_blank'</script>");
                    else
                        Response.Redirect("cust_inv_report.aspx");
                }
            }

        }
        catch
        {

        }
    }

}
