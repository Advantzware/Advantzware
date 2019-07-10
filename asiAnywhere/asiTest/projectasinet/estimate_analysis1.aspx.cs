
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

public partial class estimate_analysis1 : System.Web.UI.Page
{
    string puge = "";
    protected void Page_Load(object sender, System.EventArgs e)
    {
       
              
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "corr_print.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            Session["alpha_login"] = PrmComp;
            if (aUsers == "external")
            {
                if (!Page.IsPostBack)
                {
                    vendordiv.Visible = false;
                    Session["estimate_analysis1_vendor_no"] = "";                   

                }
            }
            if (aUsers == "internal")
            {
                if (!Page.IsPostBack)
                {
                    vendordiv.Visible = false;
                    Session["estimate_analysis1_vendor_no"] = "";

                }

            }

            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }

        if (Request.QueryString["rowcnt"] == "no")
        {
            HiddenField1.Value = "No";

            if (GridView2.Rows.Count > 0)
            {
                vendordiv.Visible = true;
            }
            else
            {
                FormView1.ChangeMode(FormViewMode.Insert);
                GridView1.Visible = true;
                Button1.Visible = true;
                close.Visible = true;
            }


            pugetable.Visible = false;
            try
            {
                SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                conn.Open();

                SqlCommand cmd_delete = new SqlCommand("delete  corr_print_maintenance where  est = '" + Convert.ToString(Session["order_corrugated_est"]) + "'", conn);
                cmd_delete.ExecuteNonQuery();
                conn.Close();
            }
            catch { }
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
    protected void SubmitButton_Click(object sender, EventArgs e)
    {
    }
    protected void FormView1_PreRender(object sender, EventArgs e)
    {
       
    }


    protected void PurgeQtyClick(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        HiddenField1.Value = "Yes";

        if (GridView2.Rows.Count > 0)
        {
            vendordiv.Visible = true;
        }
        else
        {
            FormView1.ChangeMode(FormViewMode.Insert);
            GridView1.Visible = true;
            Button1.Visible = true;
            close.Visible = true;
        }
        pugetable.Visible = false;
        try
        {
            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            conn.Open();

            SqlCommand cmd_delete = new SqlCommand("delete  corr_print_maintenance where  est = '" + Convert.ToString(Session["order_corrugated_est"]) + "'", conn);
            cmd_delete.ExecuteNonQuery();
            conn.Close();
        }
        catch { }         
    }
    protected void AppendQtyClick(object sender, EventArgs e)
    {
        HiddenField1.Value = "No";

        if (GridView2.Rows.Count > 0)
        {
            vendordiv.Visible = true;
        }
        else
        {
            FormView1.ChangeMode(FormViewMode.Insert);
            GridView1.Visible = true;
            Button1.Visible = true;
            close.Visible = true;
        }

                
        pugetable.Visible = false;
        try
        {
            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            conn.Open();

            SqlCommand cmd_delete = new SqlCommand("delete  corr_print_maintenance where  est = '" + Convert.ToString(Session["order_corrugated_est"]) + "'", conn);
            cmd_delete.ExecuteNonQuery();
            conn.Close();
        }
        catch { }
    }
    
    protected void FormView1_DataBound(object sender, EventArgs e)
    {
        
    }

    protected void Button1_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        string estimate = Convert.ToString(Session["order_corrugated_est"]);
        Int32 formno = Convert.ToInt32(Session["order_corrugated_formno"]);
        Int32 blankno = Convert.ToInt32(Session["order_corrugated_blankno"]);        

        CheckBox dogsa = (CheckBox)FormView1.FindControl("CheckBox3");
        CheckBox domr = (CheckBox)FormView1.FindControl("CheckBox2");
        CheckBox dospeed = (CheckBox)FormView1.FindControl("CheckBox1");
        CheckBox droprc = (CheckBox)FormView1.FindControl("CheckBox4");

        string do_gsa = "";
        string do_mr = "";
        string do_speed = "";
        string drop_rc = "";
        if (dogsa.Checked)
            do_gsa = "True";
        else
            do_gsa = "False";
        if (domr.Checked)
            do_mr = "True";
        else
            do_mr = "False";
        if (dospeed.Checked)
            do_speed = "True";
        else
            do_speed = "False";
        if (droprc.Checked)
            drop_rc = "True";
        else
            drop_rc = "False";


        Corrugated corr = new Corrugated();
        DataSet ds = new DataSet();
        DataTable dt = new DataTable();
        ds = corr.SelectQtyTandem(UserLogin.UserName, "doCalc", do_gsa, do_mr, do_speed, drop_rc, estimate, formno, blankno, HiddenField1.Value.Trim(),HiddenField_Vendor.Value.Trim());


        try
        {
            if (do_gsa == "True")
            {
                Session["estimate1_analysis_gsa_mat"] = ds.Tables[0].Rows[0][5].ToString();
                Session["estimate1_analysis_gsa_lab"] = ds.Tables[0].Rows[0][6].ToString();
                Session["estimate1_analysis_gsa_war"] = ds.Tables[0].Rows[0][7].ToString();
                Session["estimate1_analysis_gsa_fm"] = ds.Tables[0].Rows[0][8].ToString();
                Session["estimate1_analysis_gsa_month"] = ds.Tables[0].Rows[0][10].ToString();
                SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                conn.Open();
                int row = 0;
                int seqno = Convert.ToInt32(ds.Tables[0].Rows[0][0].ToString());
                int qty = Convert.ToInt32(ds.Tables[0].Rows[row][2].ToString());

                for (int i = 0; i < ds.Tables[0].Rows.Count; i++)
                {
                    SqlCommand cmd_insert = new SqlCommand("insert into corr_print_maintenance  values ('" + UserLogin.UserName + "','" + estimate + "','" + Convert.ToInt32(ds.Tables[0].Rows[row][0].ToString()) + "','" + Convert.ToInt32(ds.Tables[0].Rows[row][1].ToString()) + "','" + Convert.ToInt32(ds.Tables[0].Rows[row][2].ToString()) + "','" + Convert.ToString(ds.Tables[0].Rows[row][3].ToString()) + "','0','" + do_gsa + "','" + do_mr + "','" + do_speed + "','" + drop_rc + "','" + "No" + "')", conn);
                    cmd_insert.ExecuteNonQuery();
                    row = row + 1;

                }
                conn.Close();
                Response.Write("<script>location.href='corr_print_gsm2.aspx?seq=" + seqno + "&qty=" + qty + "'</script>");                                
            }
            else
            {
                Response.Write("<script>self.window.close(); window.opener.location.reload();</script>");
            }
        }
        catch { }
    }

    protected void GridView1_selectedindex_change(object sender, EventArgs e)
    {
        HiddenField_Vendor.Value = GridView2.SelectedRow.Cells[1].Text;
        Session["estimate_analysis1_vendor_no"] = GridView2.SelectedRow.Cells[1].Text;
    }
    protected void ok_button_click(object sender, EventArgs e)
    {
        vendordiv.Visible = false;
        FormView1.ChangeMode(FormViewMode.Insert);
        GridView1.Visible = true;
        Button1.Visible = true;
        close.Visible = true;
    }
    protected void Price_Button_Click(object sender, EventArgs e)
    {
        HiddenField_Vendor.Value = "bestvendor";
        Session["estimate_analysis1_vendor_no"] = "bestvendor";
        vendordiv.Visible = false;
        FormView1.ChangeMode(FormViewMode.Insert);
        GridView1.Visible = true;
        Button1.Visible = true;
        close.Visible = true;
    }
    protected void Gridview1_Sorting(object sender, GridViewSortEventArgs e)
    {

    }
}
