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

public partial class custim_report: System.Web.UI.Page
{

    private bool bSort = true;

    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
       
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "custim_report.aspx";
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
            ObjectDataSource2.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource2.SelectParameters["prmComp"].DefaultValue = Convert.ToString(Session["Customers_Company"]);


            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            try
            {
                conn.Open();

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'custim_report.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                foreach (DataRow dr in ds.Tables[0].Rows)
                {
                    TextBox1.Text          = dr["field1"].ToString();
                    BegCustTextBox.Text     = dr["field2"].ToString();
                    EndCustTextBox.Text    = dr["field3"].ToString();
                    BePoTextBox.Text   = dr["field4"].ToString();
                    EndPoTextBox.Text  = dr["field5"].ToString();
                    TextBox3.Text      = dr["field6"].ToString();
                    TextBox4.Text      = dr["field7"].ToString();
                    OlderTextBox.Text  = dr["field8"].ToString();
                    

                    if (dr["chk_field1"].ToString() == "True")
                        CheckBox1.Checked = true;
                    else
                        CheckBox1.Checked = false;
                    if (dr["chk_field2"].ToString() == "True")
                        CheckBox2.Checked = true;
                    else
                        CheckBox2.Checked = false;
                    if (dr["chk_field3"].ToString() == "True")
                        CheckBox3.Checked = true;
                    else
                        CheckBox3.Checked = false;
                    if (dr["rd_field1"].ToString() == "Order Date")
                        RadioButtonList1.SelectedIndex = 0;
                    if (dr["rd_field1"].ToString() == "Due Date")
                        RadioButtonList1.SelectedIndex = 1;

                    if (dr["rd_field2"].ToString() == "A")
                        RadioButtonList2.SelectedIndex = 0;
                    if (dr["rd_field2"].ToString() == "D")
                        RadioButtonList2.SelectedIndex = 1;

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
                if (BegCustTextBox.Text == "")
                {
                    Label custn = (Label)FormView2.FindControl("CustLabel");
                    BegCustTextBox.Text = custn.Text;
                    EndCustTextBox.Text = custn.Text;
                }
                                
                if (TextBox4.Text == "")
                    TextBox4.Text = "zzz";
                
                if (EndPoTextBox.Text == "")
                    EndPoTextBox.Text = "zzzzzzzzzzzzzzz";

                RadioButtonList_out.SelectedIndex = 0;
                TextBox1.Text = System.DateTime.Now.Date.ToShortDateString();
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
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Custim";
        ObjectDataSource1.SelectParameters["prmAsof"].DefaultValue = TextBox1.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBeCust"].DefaultValue = BegCustTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndCust"].DefaultValue = EndCustTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBePo"].DefaultValue = BePoTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndPo"].DefaultValue = EndPoTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBeSman"].DefaultValue = TextBox3.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndSman"].DefaultValue = TextBox4.Text.Trim();

        ObjectDataSource1.SelectParameters["prmOlder"].DefaultValue = OlderTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmQtyonHand"].DefaultValue =Convert.ToString(CheckBox1.Checked);
        ObjectDataSource1.SelectParameters["prmWarehouse"].DefaultValue = Convert.ToString(CheckBox2.Checked);
        ObjectDataSource1.SelectParameters["prmCustPart"].DefaultValue = Convert.ToString(CheckBox3.Checked);
        ObjectDataSource1.SelectParameters["prmOrderDue"].DefaultValue = Convert.ToString(RadioButtonList1.SelectedValue);
        ObjectDataSource1.SelectParameters["prmReceipt"].DefaultValue = Convert.ToString(RadioButtonList2.SelectedValue);
                      
        ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = RadioButtonList_out.SelectedValue;


        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        //try
        //{
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'custim_report.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name , field1, field2, field3, field4, field5, field6,field7, field8,  chk_field1,  chk_field2,  chk_field3,rd_field1,rd_field2) values ('" + UserLogin.UserName + "','custim_report.aspx' , '" + TextBox1.Text.Trim() + "', '" + BegCustTextBox.Text.Trim() + "', '" + EndCustTextBox.Text.Trim() + "', '" + BePoTextBox.Text.Trim() + "', '" + EndPoTextBox.Text.Trim() + "', '" + TextBox3.Text.Trim() + "', '" + TextBox4.Text.Trim() + "', '" + OlderTextBox.Text.Trim() + "', '" + Convert.ToString(CheckBox1.Checked) + "','" + Convert.ToString(CheckBox2.Checked) + "','" + Convert.ToString(CheckBox3.Checked) + "','" + Convert.ToString(RadioButtonList1.SelectedValue) + "','" + Convert.ToString(RadioButtonList2.SelectedValue) + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + TextBox1.Text.Trim() + "', field2 = '" + BegCustTextBox.Text.Trim() + "', field3 = '" + EndCustTextBox.Text.Trim() + "', field4 = '" + BePoTextBox.Text.Trim() + "', field5 = '" + EndPoTextBox.Text.Trim() + "', field6 = '" + TextBox3.Text.Trim() + "',field7 = '" + TextBox4.Text.Trim() + "',field8 = '" + OlderTextBox.Text.Trim() + "', chk_field1 = '" + Convert.ToString(CheckBox1.Checked) + "',chk_field2 = '" + Convert.ToString(CheckBox2.Checked) + "',chk_field3 = '" + Convert.ToString(CheckBox3.Checked) + "', rd_field1 = '" + Convert.ToString(RadioButtonList1.SelectedValue) + "',rd_field2 = '" + Convert.ToString(RadioButtonList2.SelectedValue) + "' where user_name = '" + UserLogin.UserName + "' and prog_name =  'custim_report.aspx' ", conn);
                cmd_update.ExecuteNonQuery();
            }
            conn.Close();
        //}
        //catch (Exception ex)
        //{
        //    Label1.Text = "Error :" + ex.Message + "<p>";
        //    conn.Close();
        //}
        //finally
        //{
        //    conn.Close();
        //}


        try
        {
            OutPutFile.Visible = true;
            HyperLink1.Visible = true;
            Label vpath = (Label)FormView1.FindControl("CustimReceiptLabel");
            HyperLink1.Text = vpath.Text;
            HyperLink1.NavigateUrl = @"/pdfs/" + vpath.Text;


            if (vpath.Text == "")
            {
                OutPutFile.Text = "No CSV Exists";
                
            }
        }
        catch { }       

    }

   }
